package br.unb.cdt.desafioPositivo.facade;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;

import org.jboss.seam.ScopeType;
import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.annotations.Scope;
import org.jboss.seam.core.ResourceBundle;
import org.jboss.seam.ui.util.cdk.Messages;

import com.sun.org.apache.xerces.internal.impl.dv.xs.YearDV;

import br.unb.cdt.desafioPositivo.mensagens.Mensagens;
import br.unb.cdt.desafioPositivo.model.Proposta;
import br.unb.cdt.desafioPositivo.model.Usuario;
import br.unb.cdt.desafioPositivo.model.acesso.AcessoSolicitado;
import br.unb.cdt.desafioPositivo.model.acesso.ExcecaoAcessoUsuario;
import br.unb.cdt.desafioPositivo.model.dto.AlteraSenhaDTO;
import br.unb.cdt.desafioPositivo.util.criptografia.CriptografiaUtil;
import br.unb.cdt.desafioPositivo.util.email.EmailUtil;
import br.unb.cdt.desafioPositivo.util.rest.AtualizacaoSRV;
import br.unb.cdt.desafioPositivo.util.rest.AutenticacaoSRV;
import br.unb.cdt.desafioPositivo.util.rest.CadastroSRV;
import br.unb.cdt.desafioPositivo.util.rest.CodigoRespostaAutenticacao;
import br.unb.cdt.desafioPositivo.util.rest.CodigoRespostaCadastro;
import br.unb.cdt.desafioPositivo.util.rest.CodigoRespostaNovaSenha;
import br.unb.cdt.desafioPositivo.util.rest.NovaSenhaSRV;
import br.unb.cdt.desafioPositivo.util.rest.RespostaPositivo;

@Name("facade")
@Scope(ScopeType.CONVERSATION)
@AutoCreate
/**
 * Fachada da aplicacao, disponibilizando os metodos 
 * transacionais que permitem cadastrar usuarios e enviar propostas.
 * 
 * @author positivo
 */
public class DesafioPositivoFacade {

	@In
	private EntityManager entityManager;

	@In(create=true)
	private EmailUtil emailUtil;

	@In(create=true)
	private String urlConfirmacaoCadastro;

	@In
	private Map<String, String> messages;

	/**
	 * Adiciona um usuario no meio de persistencia e realiza uma requisicao ao
	 * servico correspondente da positivo.
	 * 
	 * @param dto
	 *            usuario a ser cadastrado
	 * @throws Exception
	 *             Caso algum problema tenha ocorrido.
	 */
	public void adicionarUsuario(Usuario dto) throws ExcecaoUsuarioCadastrado,
	ExcecaoEnvioEmail, Exception {
		AutenticacaoSRV autentica = new AutenticacaoSRV(dto.getEmail(),
		"123456");

		autentica.preparaRequisicao();
		int resp = autentica.requisitaServico().getCodigo();

		switch (CodigoRespostaAutenticacao.fromCodigo(resp)) {
		case CLIENTE_NAO_ENCONTRADO:
			cadastraNovoUsuario(dto);
			break;

		case SENHA_INVALIDA:
			throw new ExcecaoUsuarioCadastrado();

		case SUCESSO:
			throw new ExcecaoUsuarioCadastrado();

		default:
			throw new Exception(Mensagens.EXP_CADASTRO);
		}
	}

	/**
	 * Atualiza os dados do usuario no meio de persistencia e realiza uma
	 * requisicao ao servico correspondente da Positivo.
	 * 
	 * @param dto
	 * @throws Exception
	 */
	public void atualizarUsuario(Usuario usuarioLogado) throws Exception {
		try {
			Usuario usuario = recuperaUsuario(usuarioLogado.getEmail());

			if (usuario == null) {
				throw new Exception(Mensagens.EXP_REQUISICAO);
			}
			
			validaDados(usuarioLogado);

			usuario.setNome(usuarioLogado.getNome());
			usuario.setSobrenome(usuarioLogado.getSobrenome());
			usuario.setSexo(usuarioLogado.getSexo());
			usuario.setNascimento(usuarioLogado.getNascimento());
			usuario.setCep(usuarioLogado.getCep());
			usuario.setBairro(usuarioLogado.getBairro());
			usuario.setEndereco(usuarioLogado.getEndereco());
			usuario.setEstado(usuarioLogado.getEstado());

			AtualizacaoSRV srv = new AtualizacaoSRV(usuario);

			srv.preparaRequisicao();
			RespostaPositivo resp = srv.requisitaServico();

			switch (CodigoRespostaCadastro.fromCodigo(resp.getCodigo())) {
			case SUCESSO:
				usuario.setToken(resp.getToken());
				entityManager.merge(usuario);
				entityManager.flush();
				break;

			default:
				throw new Exception(Mensagens.EXP_REQUISICAO);
			}
		} catch (Exception e) {
			e.printStackTrace();
			throw new Exception(Mensagens.EXP_REQUISICAO);
		}
	}

	public void alterarSenha(Usuario usuarioLogado, AlteraSenhaDTO senha)
	throws Exception {
		// verifica se a senha atual informada eh a correta, procedendo
		// com uma autenticacao.
		AutenticacaoSRV autenticacaoSrv = new AutenticacaoSRV(
				usuarioLogado.getEmail(), senha.getSenhaAtual());

		autenticacaoSrv.preparaRequisicao();

		RespostaPositivo resp = autenticacaoSrv.requisitaServico();

		switch (CodigoRespostaAutenticacao.fromCodigo(resp.getCodigo())) {
		case SUCESSO:
			realizaAlteracaoSenha(usuarioLogado, senha);
			break;
		case SENHA_INVALIDA:
			throw new Exception(Mensagens.EXP_SENHA);

		default:
			throw new Exception(Mensagens.EXP_REQUISICAO);
		}
	}

	private void realizaAlteracaoSenha(Usuario usuarioLogado,
			AlteraSenhaDTO senha) throws Exception {
		Usuario usuario = recuperaUsuario(usuarioLogado.getEmail());

		NovaSenhaSRV novaSenhaSrv = new NovaSenhaSRV(usuario.getToken(),
				senha.getNovaSenha());

		novaSenhaSrv.preparaRequisicao();

		RespostaPositivo resp = novaSenhaSrv.requisitaServico();

		switch (CodigoRespostaNovaSenha.fromCodigo(resp.getCodigo())) {
		case SUCESSO:
			usuario.setToken(resp.getToken());
			entityManager.merge(usuario);
			entityManager.flush();
			break;
		default:
			throw new Exception(Mensagens.EXP_REQUISICAO);
		}
	}

	/**
	 * Confirma a solicitacao de cadastro.
	 * 
	 * @param dto
	 *            dados da confirmacao de solicitacao de cadastro
	 */
	public void confirmarSolicitacaoCadstro(Usuario dto) throws ExcecaoUsuarioNaoEncontrado, Exception {
		Usuario usuario = recuperaUsuario(dto.getEmail());

		if (usuario != null) {
			usuario.setSenha(dto.getSenha());

			CadastroSRV srv = new CadastroSRV(usuario);
			srv.preparaRequisicao();
			RespostaPositivo resp = srv.requisitaServico();

			switch (CodigoRespostaCadastro.fromCodigo(resp.getCodigo())) {
			case SUCESSO:
				confirmaCadastro(dto, usuario, resp);
				break;

			case CLIENTE_JA_EXISTE:
				// existe na positivo, mas continua pendente aqui
				if (usuario.getSituacaoAcessoAtual().getClass()
						.equals(AcessoSolicitado.class)) {
					confirmaCadastro(dto, usuario, resp);
				} else {
					throw new ExcecaoUsuarioCadastrado();
				}

			default:
				throw new Exception(Mensagens.EXP_CONFIRMACAO_CADASTRO);
			}

		} else {
			throw new ExcecaoUsuarioNaoEncontrado();
		}
	}

	/*
	 * Persiste um novo usuario na base de dados.
	 */
	private void cadastraNovoUsuario(Usuario usuario) throws ExcecaoEnvioEmail, Exception {
		AcessoSolicitado acesso = new AcessoSolicitado();

		validaDados(usuario);
		acesso.setUsuario(usuario);
		acesso.setCodigoEfetivacao(geraCodigoConfirmacaoCadastro(usuario));

		emailUtil.enviarEmail(new String[] {usuario.getEmail()} , messages.get(Mensagens.FACADE_NOVO_USUARIO), mensagemCadastro(usuario, acesso.getCodigoEfetivacao()));

		usuario.getHistoricoSituacaoAcesso().add(acesso);

		entityManager.merge(usuario);
		entityManager.flush();
	}

	private void validaDados(Usuario usuario) throws ExcecaoNomeInvalido, ExcecaoIdadeInvalida, ExcecaoSobrenomeInvalido {

		// Verifica nome
		if (usuario.getNome().contains("!") || 
				usuario.getNome().contains("@") || 
				usuario.getNome().contains("#") ||
				usuario.getNome().contains("$") || 
				usuario.getNome().contains("%") || 
				usuario.getNome().contains("�") || 
				usuario.getNome().contains("&") || 
				usuario.getNome().contains("*") || 
				usuario.getNome().contains("(") || 
				usuario.getNome().contains(")") || 
				usuario.getNome().contains("-") || 
				usuario.getNome().contains("_") || 
				usuario.getNome().contains("+") || 
				usuario.getNome().contains("=") || 
				usuario.getNome().contains("�") || 
				usuario.getNome().contains("[") || 
				usuario.getNome().contains("{") || 
				usuario.getNome().contains("]") || 
				usuario.getNome().contains("}") || 
				usuario.getNome().contains(";") || 
				usuario.getNome().contains(":") || 
				usuario.getNome().contains(".") || 
				usuario.getNome().contains(",") || 
				usuario.getNome().contains(">") || 
				usuario.getNome().contains("<") || 
				usuario.getNome().contains("0") ||
				usuario.getNome().contains("1") ||
				usuario.getNome().contains("2") ||
				usuario.getNome().contains("3") ||
				usuario.getNome().contains("4") ||
				usuario.getNome().contains("5") ||
				usuario.getNome().contains("6") ||
				usuario.getNome().contains("7") ||
				usuario.getNome().contains("8") ||
				usuario.getNome().contains("9")) {
			throw new ExcecaoNomeInvalido();
		}
		
		// Verifica sobrenome
		if (usuario.getSobrenome().contains("!") || 
				usuario.getSobrenome().contains("@") || 
				usuario.getSobrenome().contains("#") ||
				usuario.getSobrenome().contains("$") || 
				usuario.getSobrenome().contains("%") || 
				usuario.getSobrenome().contains("�") || 
				usuario.getSobrenome().contains("&") || 
				usuario.getSobrenome().contains("*") || 
				usuario.getSobrenome().contains("(") || 
				usuario.getSobrenome().contains(")") || 
				usuario.getSobrenome().contains("-") || 
				usuario.getSobrenome().contains("_") || 
				usuario.getSobrenome().contains("+") || 
				usuario.getSobrenome().contains("=") || 
				usuario.getSobrenome().contains("�") || 
				usuario.getSobrenome().contains("[") || 
				usuario.getSobrenome().contains("{") || 
				usuario.getSobrenome().contains("]") || 
				usuario.getSobrenome().contains("}") || 
				usuario.getSobrenome().contains(";") || 
				usuario.getSobrenome().contains(":") || 
				usuario.getSobrenome().contains(".") || 
				usuario.getSobrenome().contains(",") || 
				usuario.getSobrenome().contains(">") || 
				usuario.getSobrenome().contains("<") || 
				usuario.getSobrenome().contains("0") ||
				usuario.getSobrenome().contains("1") ||
				usuario.getSobrenome().contains("2") ||
				usuario.getSobrenome().contains("3") ||
				usuario.getSobrenome().contains("4") ||
				usuario.getSobrenome().contains("5") ||
				usuario.getSobrenome().contains("6") ||
				usuario.getSobrenome().contains("7") ||
				usuario.getSobrenome().contains("8") ||
				usuario.getSobrenome().contains("9")) {
			throw new ExcecaoSobrenomeInvalido();
		}
		
		// Calcula a idade
		Calendar now = Calendar.getInstance();
		Calendar data = Calendar.getInstance();
		data.setTime(usuario.getNascimento());
		int idade = now.get(Calendar.YEAR) - data.get(Calendar.YEAR);  
		if (now.get(Calendar.MONTH) < data.get(Calendar.MONTH)) {
		  idade--;  
		} else if (now.get(Calendar.MONTH) == data.get(Calendar.MONTH)
		    && now.get(Calendar.DAY_OF_MONTH) < data.get(Calendar.DAY_OF_MONTH)) {
		  idade--;  
		}
		
		// Verifica idade
		if(idade < 12) {
			throw new ExcecaoIdadeInvalida();
		}
	}

	private String mensagemCadastro(Usuario usuario, String codigoAtivacao) {
		return  messages.get(Mensagens.MSG_WELCOME) + ", " + usuario.getNome() + 
		", \n \n \n" + 
		messages.get(Mensagens.MSG_BODY) + "\n\n\n" + urlConfirmacaoCadastro + " \n \n \n" + 
		messages.get(Mensagens.MSG_ACCESS_CODE) + "\n\n\n" + codigoAtivacao +
		"\n \n \n" +
		messages.get(Mensagens.MSG_END) + ", \n" +
		messages.get(Mensagens.MSG_ATT) + ".";
		/*		
		return "Prezado " + usuario.getNome() + 
				", \n \n \n" + 
			   "Para confirmar o seu acesso ao sistema, acesse a URL " + urlConfirmacaoCadastro + 
			   "\n \n \n e entre com o seguinte codigo: \n \n \n" + codigoAtivacao + 
			   "\n \n \n" +
			   "Atenciosamente, \n" +
			   "Coordenacao do desafio positivo. ";
		 */
	}

	/**
	 * Autentica um usuario requisitando um servico da Positivo.
	 * 
	 * @param email
	 *            do usuario
	 * @param senha
	 *            do usuario
	 * @return o usuario autenticado
	 * @throws Exception
	 *             caso ocorra algum problema na excecao
	 */
	public Usuario autenticarUsuario(String email, String senha)
	throws ExcecaoFalhaAutenticacao, ExcecaoAcessoUsuario, Exception {
		RespostaPositivo resp = autenticarNaRedePositivo(email, senha);

		Usuario usuario = recuperaUsuario(email);

		if (usuario == null) {
			throw new ExcecaoFalhaAutenticacao(Mensagens.EXP_USUARIO_SENHA);
		} else {
			usuario.getSituacaoAcessoAtual().autenticar(resp.getCodigo() == 0);
		}

		switch (CodigoRespostaAutenticacao.fromCodigo(resp.getCodigo())) {
		case SUCESSO:
			return recuperaUsuario(email);

		case SENHA_INVALIDA:
			throw new ExcecaoFalhaAutenticacao(Mensagens.EXP_USUARIO_SENHA);

		case CLIENTE_NAO_ENCONTRADO:
			throw new ExcecaoFalhaAutenticacao(Mensagens.EXP_USUARIO_SENHA);

		default:
			throw new Exception(Mensagens.EXP_AUTENTICACAO);
		}
	}

	/**
	 * Adiciona uma proposta submetida pelo usuario logado.
	 * 
	 * @param usuario
	 *            usuario logado no sistema
	 * @param proposta
	 *            proposta submetida
	 */
	public void adicionarProposta(Usuario usuarioLogado, Proposta proposta) {
		if (usuarioLogado.getPropostas() == null) {
			usuarioLogado.setPropostas(new ArrayList<Proposta>());
		}

		usuarioLogado.getPropostas().add(proposta);
		//proposta.setArquivoGUI(fileUploadBean.getFiles().get(0).getData());
		proposta.setUsuario(usuarioLogado);

		Usuario u = entityManager.merge(usuarioLogado);
		entityManager.flush();

		int index = usuarioLogado.getPropostas().indexOf(proposta);
		Proposta antiga = usuarioLogado.getPropostas().get(index);
		Proposta atualizada = u.getPropostas().get(index);

		antiga.setId(atualizada.getId());
	}

	/**
	 * Utilizando contexto transacional, recupera as propostas submetidas pelo
	 * usuario e que podem nao ter sido previamente carregadas (uso da
	 * propriedade Lazy).
	 * 
	 * @param usuarioLogado
	 *            usuario logado no sistema
	 * @return propostas submetidas pelo usuario
	 */
	public List<Proposta> recuperaPropostas(Usuario usuarioLogado) {
		return usuarioLogado.getPropostas();
	}


	/*
	 * Confirma o cadastro do usuario na base de dados local.
	 */
	private void confirmaCadastro(Usuario dto, Usuario usuario,
			RespostaPositivo resp) throws ExcecaoAcessoUsuario {
		usuario.setToken(resp.getToken());
		usuario.getSituacaoAcessoAtual().confirmarCadastro(
				dto.getCodigoConfirmacaoCadastro());
		entityManager.merge(usuario);
		entityManager.flush();
	}

	/*
	 * Realiza a autenticacao na rede Positivo.
	 */
	private RespostaPositivo autenticarNaRedePositivo(String email, String senha)
	throws Exception {
		AutenticacaoSRV req = new AutenticacaoSRV(email, senha);
		req.preparaRequisicao();
		RespostaPositivo resp = req.requisitaServico();
		return resp;
	}

	/*
	 * Gera um codigo para ser usado na confirmacao do cadastro.
	 */
	private String geraCodigoConfirmacaoCadastro(Usuario usuario)
	throws Exception {
		Date dataAtual = Calendar.getInstance().getTime();
		String codigo = CriptografiaUtil.criptografarMD5(usuario.getEmail()
				+ dataAtual.toString());
		return codigo;
	}

	/*
	 * Gera uma nova senha para o usuario, utilizando um hash.
	 */
	private String geraSenha(Usuario dto) throws java.lang.Exception {
		String email = dto.getEmail();
		String date = Calendar.getInstance().getTime().toString();

		String senha = CriptografiaUtil.criptografarMD5(email + date)
		.substring(0, 6);

		return senha.toUpperCase();
	}

	/*
	 * Recupera um usuario na base de dados pelo email. Nessa arquitetura,
	 * optamos por nao fazer uso do padrao Data Access Objects, dada a
	 * simplicidade do projeto.
	 */
	private Usuario recuperaUsuario(String email) throws Exception {
		try {
			@SuppressWarnings("unchecked")
			List<Usuario> usuarios = entityManager
			.createQuery("FROM Usuario u where u.email = :pEmail")
			.setParameter("pEmail", email).getResultList();

			if (usuarios == null || usuarios.size() == 0) {
				return null;
			} else
				return usuarios.get(0);
		} catch (Exception e) {
			throw new Exception(Mensagens.EXP_CONSULTA_USUARIO);
		}
	}

	/**
	 * Recupera a senha do usuario, fazendo uma requisicao ao servico da
	 * Positivo para atualizacao de senha.
	 */
	public void recuperarSenha(Usuario dto) throws ExcecaoAcessoUsuario,
	ExcecaoUsuarioNaoEncontrado, Exception {
		Usuario usuario = recuperaUsuario(dto.getEmail());

		if (usuario != null) {
			usuario.getSituacaoAcessoAtual().alterarSenha();
			String novaSenha = geraSenha(dto);

			NovaSenhaSRV servico = new NovaSenhaSRV(usuario.getToken(),
					novaSenha);

			servico.preparaRequisicao();

			RespostaPositivo resp = servico.requisitaServico();

			switch (CodigoRespostaNovaSenha.fromCodigo(resp.getCodigo())) {
			case SUCESSO:
				emailUtil.enviarEmail(new String[] {usuario.getEmail()}, messages.get(Mensagens.FACADE_ALTERA_SENHA), mensagemAlteracaoSenha(usuario, novaSenha));
				usuario.setToken(resp.getToken());
				entityManager.merge(usuario);
				entityManager.flush();
				break;

			case SENHA_INVALIDA:
				throw new Exception(Mensagens.EXP_SENHA_GERADA_INVALIDA);

			case CLIENTE_NAO_EXISTE:
				throw new ExcecaoUsuarioNaoEncontrado();

			case OUTROS:
				throw new Exception(Mensagens.EXP_TRANSACAO);
			}
		} else {
			throw new ExcecaoUsuarioNaoEncontrado();
		}
	}

	private String mensagemAlteracaoSenha(Usuario usuario, String novaSenha) {
		/*
		return Mensagens.MSG_WELCOME + ", " + usuario.getNome() + 
				", \n \n \n" + 
				Mensagens.MSG_BODY_SENHA + ": \n \n \n" + novaSenha +
				"\n \n \n" +
				Mensagens.MSG_END + ", \n" +
				Mensagens.MSG_ATT + ".";
		 */

		return  messages.get(Mensagens.MSG_WELCOME) + ", " + usuario.getNome() + 
		", \n \n \n" + 
		messages.get(Mensagens.MSG_BODY_SENHA) + "\n\n\n" + novaSenha + 
		"\n \n \n" +
		messages.get(Mensagens.MSG_END) + ", \n" +
		messages.get(Mensagens.MSG_ATT) + ".";
	}

	public void excluirProposta(Proposta propostaSelecionada) throws Exception {
		Proposta proposta = recuperaProposta(propostaSelecionada.getId());
		entityManager.remove(proposta);
	}

	private Proposta recuperaProposta(Long id) throws Exception {
		try {
			@SuppressWarnings("unchecked")
			List<Proposta> propostas = entityManager
			.createQuery("FROM Proposta p where p.id = :pId")
			.setParameter("pId", id).getResultList();

			if (propostas == null || propostas.size() == 0) {
				return null;
			} else
				return propostas.get(0);
		} catch (Exception e) {
			throw new Exception(Mensagens.EXP_RECUPERA_PROPOSTA);
		}
	}


	public void editarProposta(Proposta propostaSelecionada) throws Exception {
		Proposta proposta = recuperaProposta(propostaSelecionada.getId());
		if(proposta == null) {
			throw new Exception(Mensagens.EXP_REQUISICAO);
		}

		proposta.setNome(propostaSelecionada.getNome());
		proposta.setDescricao(propostaSelecionada.getDescricao());
		proposta.setObjetivos(propostaSelecionada.getObjetivos());
		proposta.setPublicoAlvo(propostaSelecionada.getPublicoAlvo());
		proposta.setDescricaoFuncional(propostaSelecionada.getDescricaoFuncional());

		entityManager.merge(proposta);
		entityManager.flush();
	}

	public Usuario recuperaUsuarioCPF(String cpf) throws Exception {
		try {
			@SuppressWarnings("unchecked")
			List<Usuario> usuarios = entityManager
			.createQuery("FROM Usuario u where u.cpf = :pCpf")
			.setParameter("pCpf", cpf).getResultList();

			if (usuarios == null || usuarios.size() == 0) {
				return null;
			} else
				return usuarios.get(0);
		} catch (Exception e) {
			throw new Exception(Mensagens.EXP_CONSULTA_USUARIO);
		}
	}

}
