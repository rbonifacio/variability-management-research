package br.unb.cdt.desafioPositivo.facade;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
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
import org.jboss.seam.util.RandomStringUtils;

import br.unb.cdt.desafioPositivo.mensagens.Mensagens;
import br.unb.cdt.desafioPositivo.model.Estado;
import br.unb.cdt.desafioPositivo.model.Proposta;
import br.unb.cdt.desafioPositivo.model.Sexo;
import br.unb.cdt.desafioPositivo.model.Usuario;
import br.unb.cdt.desafioPositivo.model.acesso.AcessoAtivo;
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
import br.unb.cdt.desafioPositivo.util.rest.CodigoRespostaEnviaEmail;
import br.unb.cdt.desafioPositivo.util.rest.CodigoRespostaEsqueciSenha;
import br.unb.cdt.desafioPositivo.util.rest.CodigoRespostaNovaSenha;
import br.unb.cdt.desafioPositivo.util.rest.ConsultaClienteSRV;
import br.unb.cdt.desafioPositivo.util.rest.EnviaEmailSRV;
import br.unb.cdt.desafioPositivo.util.rest.EsqueciSenhaSRV;
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
	
	@In(create=true)
	private String urlRecuperarSenhaPositivo;

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
				geraSenhaPseudoAleatoria());

		autentica.preparaRequisicao();
		int resp = autentica.requisitaServico().getCodigo();

		switch (CodigoRespostaAutenticacao.fromCodigo(resp)) {
		case CLIENTE_NAO_ENCONTRADO:
			cadastraNovoUsuario(dto);
			break;

		case SENHA_INVALIDA:
			emailUtil.enviarEmailLog("[ERRO-GRAVE] Cadastro Usuário: Usuário já existia", 
				"Erro ao inserir usuario. Usuario: \n" + dto.toLog() );
			throw new ExcecaoUsuarioCadastrado();

		case SUCESSO:
			emailUtil.enviarEmailLog("[ERRO-GRAVE] Cadastro Usuário: Usuário já existia", 
					"Erro ao inserir usuario. Usuario: \n" + dto.toLog() );
			throw new ExcecaoUsuarioCadastrado();

		default:
			emailUtil.enviarEmailLog("[ERRO-GRAVE] Cadastro Usuário: Caso Default", 
					"Não houve resultado interpretavel na autenticacao" );
			throw new Exception(Mensagens.EXP_CADASTRO);
		}
	}

	
	/*
	 * esse metodo eh usado para gerar uma senha 
	 * pseudo aleatorio para ser usada na tentativa 
	 * de autenticacao do usuario durante a operacao 
	 * de cadastro.
	 */
	private String geraSenhaPseudoAleatoria() throws Exception {
		return RandomStringUtils.randomAlphanumeric(5);  
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

			usuario.setCpf(usuarioLogado.getCpf());
			usuario.setRg(usuarioLogado.getRg());

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
			throw e;
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
			throw new ExcecaoSenhaIncorreta();

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
					emailUtil.enviarEmailLog("[EXCEPCIONAL] Confirmação Usuario: " +
							"Existe na positivo, porém continua pendente no concurso ideapp", 
							"Cadastro confirmado com sucesso. Usuario: \n" + dto.toLog() );
				} else {
					emailUtil.enviarEmailLog("[ERRO-GRAVE] Confirmação Usuario: " +
							"Existe na positivo, porém continua pendente no concurso ideapp", 
							"Erro ao inserir usuario. Usuario: \n" + dto.toLog() );
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

	//TODO: Ateh agora eu nao entendi o porque disso. 
	//hilmer tinha me falado que isso era codigo morto. 
	//tentei remover e nao tem nada de codigo morto!
	private void validaDados(Usuario usuario) throws ExcecaoNomeInvalido, ExcecaoIdadeInvalida, ExcecaoSobrenomeInvalido {

		// Verifica nome
		if (usuario.getNome().contains("!") || 
				usuario.getNome().contains("@") || 
				usuario.getNome().contains("#") ||
				usuario.getNome().contains("$") || 
				usuario.getNome().contains("%") || 
				usuario.getNome().contains("ï¿½") || 
				usuario.getNome().contains("&") || 
				usuario.getNome().contains("*") || 
				usuario.getNome().contains("(") || 
				usuario.getNome().contains(")") || 
				usuario.getNome().contains("-") || 
				usuario.getNome().contains("_") || 
				usuario.getNome().contains("+") || 
				usuario.getNome().contains("=") || 
				usuario.getNome().contains("ï¿½") || 
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
				usuario.getSobrenome().contains("ï¿½") || 
				usuario.getSobrenome().contains("&") || 
				usuario.getSobrenome().contains("*") || 
				usuario.getSobrenome().contains("(") || 
				usuario.getSobrenome().contains(")") || 
				usuario.getSobrenome().contains("-") || 
				usuario.getSobrenome().contains("_") || 
				usuario.getSobrenome().contains("+") || 
				usuario.getSobrenome().contains("=") || 
				usuario.getSobrenome().contains("ï¿½") || 
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

	}

	private String mensagemCadastro(Usuario usuario, String codigoAtivacao) {
		return  messages.get(Mensagens.MSG_WELCOME) + ", " + usuario.getNome() + 
				", \n \n \n" + 
				messages.get(Mensagens.MSG_BODY) + "\n\n\n" + urlConfirmacaoCadastro + " \n \n \n" + 
				messages.get(Mensagens.MSG_ACCESS_CODE) + "\n\n\n" + codigoAtivacao +
				"\n \n \n" +
				messages.get(Mensagens.MSG_END) + ", \n" +
				messages.get(Mensagens.MSG_ATT) + ".";
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

		if (verificaUsuarioPositivo(resp, usuario)) {
			cadastroUsuarioPositivo(resp);
		} else if(usuario == null) {
			emailUtil.enviarEmailLog("[ERRO-BAIXO] Autenticar Usuario: " +
					"Usuario Inexistente", "Erro ao inserir usuario. E-mail: " + email );
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

	private boolean verificaUsuarioPositivo(RespostaPositivo resp,
			Usuario usuario) {
		return (usuario == null) && (CodigoRespostaAutenticacao.fromCodigo(resp.getCodigo()) == CodigoRespostaAutenticacao.SUCESSO);
	}

	private void cadastroUsuarioPositivo(RespostaPositivo resp)
			throws ExcecaoAcessoUsuario {
		Usuario usuario = new Usuario();
		System.out.println("-- Cenário Usuário POSITIVO --");
		try {
			String token = resp.getToken();
			ConsultaClienteSRV req = new ConsultaClienteSRV(token);
			req.preparaRequisicao();
			RespostaPositivo resp2 = req.requisitaServico();
			usuario.setNome(resp2.getNome());
			usuario.setSobrenome(resp2.getSobrenome());				 	
			usuario.setNascimento(calculaDataNascimento(resp2.getDataNascimento()).getTime());
			usuario.setSexo(resp2.getSexo().equals("M") ? Sexo.MASCULINO : Sexo.FEMININO);
			usuario.setEstado(Estado.valueOf(resp2.getEstado()));
			usuario.setEmail(resp2.getEmail());
			usuario.setToken(resp2.getToken());
			usuario.setConfirmacaoEmail(resp2.getEmail());

			AcessoAtivo acesso = new AcessoAtivo();
			acesso.setUsuario(usuario);

			usuario.getHistoricoSituacaoAcesso().add(acesso);

			entityManager.merge(usuario);
			entityManager.flush();
		} catch(NullPointerException e) {
			e.printStackTrace();

			emailUtil.enviarEmailLog("[ERRO-GRAVE] Usuario positivo: NULL POINTER", 
					"Erro ao inserir usuario. Usuario: \n" + usuario.toLog() + 
					"\nSTACK TRACE: \n\n" + e.getLocalizedMessage() + e.getMessage() );

			System.out.println("-- Cenário Usuário POSITIVO FALHOU | NULL POINTER | --");
			throw new ExcecaoAcessoUsuario("Importação de usuário Positivo falhou!");
		} catch(Exception e) {
			e.printStackTrace();
			
			emailUtil.enviarEmailLog("[ERRO-GRAVE] Usuario positivo: Exception mal definida", 
					"Erro ao inserir usuario. Usuario: \n" + usuario.toLog() + 
					"\nSTACK TRACE: \n\n" + e.getLocalizedMessage() + e.getMessage() );
			
			throw new ExcecaoAcessoUsuario("Importação de usuário Positivo falhou!");
		}
	}

	private Calendar calculaDataNascimento(String nascimento) {
		Calendar c = Calendar.getInstance();

		int dia = Integer.parseInt(nascimento.substring(0, 2));
		int mes = Integer.parseInt(nascimento.substring(3, 5));
		int ano = Integer.parseInt(nascimento.substring(6, 10));
		c.set(Calendar.YEAR, ano);
		c.set(Calendar.MONTH, mes);
		c.set(Calendar.DAY_OF_MONTH, dia);
		return c;
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
				.substring(0, 10);

		return senha.toUpperCase();
	}

	/*
	 * Recupera um usuario na base de dados pelo email. Nessa arquitetura,
	 * optamos por nao fazer uso do padrao Data Access Objects, dada a
	 * simplicidade do projeto.
	 */
	public Usuario recuperaUsuario(String email) throws Exception {
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


	private byte[] gerarHash(String string, String algorithm) {
		try {
			MessageDigest md = MessageDigest.getInstance(algorithm);
			md.update(string.getBytes());
			return md.digest();
		} catch (NoSuchAlgorithmException e) {
			return null;
		}
	}

	private static String stringHex(byte[] bytes) {
		StringBuilder s = new StringBuilder();
		for (int i = 0; i < bytes.length; i++) {
			int high = ((bytes[i] >> 4) & 0xf) << 4;
			int low = bytes[i] & 0xf;
			if (high == 0)
				s.append('0');
			s.append(Integer.toHexString(high | low));
		}
		return s.toString();
	}

	private String gerarSenhaAleatoria() {
		Calendar c = Calendar.getInstance();
		return stringHex(gerarHash(c.toString(), "MD5")).substring(0, 21);
	}


	/**
	 * Recupera a senha do usuario, fazendo uma requisicao ao servico da
	 * Positivo para atualizacao de senha.
	 */
	public String recuperarSenha(Usuario dto) throws ExcecaoAcessoUsuario,
	ExcecaoUsuarioNaoEncontrado, Exception {
		Usuario usuario = recuperaUsuario(dto.getEmail());
		if (usuario != null) {
			System.out.println("-- Usuario cadastrado localmente --");
			System.out.println("-- Recuperando senha... Fluxo Básico --");
			recuperaSenhaFB(dto, usuario);
			return "";
		} else {
			System.out.println("-- Usuario não cadastrado localmente --");
			System.out.println("-- Recuperando senha... Fluxo Alternativo --");
			return recuperaSenhaFA(dto);
		}
	}

	/**
	 * Esse metodo esta estremamenta estranho =/
	 * @param dto
	 * @return
	 * @throws Exception
	 * @throws ExcecaoEnvioEmail
	 * @throws ExcecaoUsuarioNaoEncontrado
	 */
	private String recuperaSenhaFA(Usuario dto) throws Exception,
			ExcecaoEnvioEmail, ExcecaoUsuarioNaoEncontrado {
		RespostaPositivo resp;
		
		if( !(dto.getTicket() == null) && !(dto.getTicket().equals(""))) {
			
			verificaSenhasInformadas(dto.getSenha(), dto.getConfirmacaoSenha());
			
			EsqueciSenhaSRV esqueciSenha = new EsqueciSenhaSRV(dto.getTicket(), dto.getSenha());
			esqueciSenha.preparaRequisicao();
			resp = esqueciSenha.requisitaServico();
			switch(CodigoRespostaEsqueciSenha.fromCodigo(resp.getCodigo())) {
			case SUCESSO:
				return "SENHA_REDEFINIDA";
			case TICKET_INVALIDO:
				emailUtil.enviarEmailLog("[ERRO-GRAVE] Recuperar Senha FA: Ticket Inválido", 
						"Erro ao recuperar senha. Usuario: \n" + dto.toLog() );
				throw new Exception("Ticket incorreto.");
			default:
				throw new Exception("Ocorreu um erro inesperado.");
			}

		}
		
		do {
			AutenticacaoSRV autentica = new AutenticacaoSRV(dto.getEmail(), gerarSenhaAleatoria());
			autentica.preparaRequisicao();
			resp = autentica.requisitaServico();
		} while(CodigoRespostaAutenticacao.fromCodigo(resp.getCodigo()) == CodigoRespostaAutenticacao.SUCESSO);

		if(CodigoRespostaAutenticacao.fromCodigo(resp.getCodigo()) == CodigoRespostaAutenticacao.SENHA_INVALIDA) {
			EnviaEmailSRV enviaEmail = new EnviaEmailSRV(dto.getEmail(), urlRecuperarSenhaPositivo);
			enviaEmail.preparaRequisicao();
			resp = enviaEmail.requisitaServico();
		}
		
		if(CodigoRespostaEnviaEmail.fromCodigo(resp.getCodigo()) != CodigoRespostaEnviaEmail.SUCESSO) {
			emailUtil.enviarEmailLog("[ERRO-GRAVE] Recuperar Senha FA: Falha no envio do email", 
					"Falha ao enviar email de recuperacao de senha: \n" + dto.toLog() );
			throw new ExcecaoEnvioEmail("Ocorreu um erro ao enviar o e-mail. Tente novamente mais tarde.");
		}
		
		return "EMAIL_ENVIADO";
		
	}

	private void recuperaSenhaFB(Usuario dto, Usuario usuario)
			throws ExcecaoAcessoUsuario, Exception, ExcecaoUsuarioNaoEncontrado {
		RespostaPositivo resp;
		usuario.getSituacaoAcessoAtual().alterarSenha();
		String novaSenha = geraSenha(dto);

		NovaSenhaSRV servico = new NovaSenhaSRV(usuario.getToken(),
				novaSenha);

		servico.preparaRequisicao();

		resp = servico.requisitaServico();

		switch (CodigoRespostaNovaSenha.fromCodigo(resp.getCodigo())) {
		case SUCESSO:
			emailUtil.enviarEmail(new String[] {usuario.getEmail()}, messages.get(Mensagens.FACADE_ALTERA_SENHA), mensagemAlteracaoSenha(usuario, novaSenha));
			usuario.setToken(resp.getToken());
			entityManager.merge(usuario);
			entityManager.flush();
			break;

		case SENHA_INVALIDA:
			recuperarSenha(dto); /* chamada recursiva, atÃ© uma senha vÃ¡lida ser gerada */
			//throw new Exception(Mensagens.EXP_SENHA_GERADA_INVALIDA);

		case CLIENTE_NAO_EXISTE:
			throw new ExcecaoUsuarioNaoEncontrado();

		case OUTROS:
			throw new Exception(Mensagens.EXP_TRANSACAO);
		}
	}
	
	private void verificaSenhasInformadas(String senha, String confirmacao)
			throws ExcecaoSenhaInvalida, ExcecaoSenhaDiferente {
		if (!CriptografiaUtil.verificaSenha(senha)) {
			throw new ExcecaoSenhaInvalida();
		}

		if (!senha.equals(confirmacao)) {
			throw new ExcecaoSenhaDiferente();
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

		if(propostaSelecionada.getArquivoGUI().length != 0){
			proposta.setArquivoGUI(propostaSelecionada.getArquivoGUI());
			proposta.setNomeArquivo(propostaSelecionada.getNomeArquivo());
		}

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
