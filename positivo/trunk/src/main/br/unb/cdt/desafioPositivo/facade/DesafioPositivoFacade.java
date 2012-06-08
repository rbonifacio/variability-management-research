package br.unb.cdt.desafioPositivo.facade;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import javax.persistence.EntityManager;

import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.faces.Renderer;

import br.unb.cdt.desafioPositivo.fileUpload.FileUploadBean;
import br.unb.cdt.desafioPositivo.model.Proposta;
import br.unb.cdt.desafioPositivo.model.Usuario;
import br.unb.cdt.desafioPositivo.model.acesso.AcessoSolicitado;
import br.unb.cdt.desafioPositivo.model.acesso.ExcecaoAcessoUsuario;
import br.unb.cdt.desafioPositivo.model.dto.AlteraSenhaDTO;
import br.unb.cdt.desafioPositivo.util.criptografia.CriptografiaUtil;
import br.unb.cdt.desafioPositivo.util.rest.AtualizacaoSRV;
import br.unb.cdt.desafioPositivo.util.rest.AutenticacaoSRV;
import br.unb.cdt.desafioPositivo.util.rest.CadastroSRV;
import br.unb.cdt.desafioPositivo.util.rest.CodigoRespostaAutenticacao;
import br.unb.cdt.desafioPositivo.util.rest.CodigoRespostaCadastro;
import br.unb.cdt.desafioPositivo.util.rest.CodigoRespostaNovaSenha;
import br.unb.cdt.desafioPositivo.util.rest.NovaSenhaSRV;
import br.unb.cdt.desafioPositivo.util.rest.RespostaPositivo;


@Name("facade")
@AutoCreate
/**
 * Fachada da aplicacao, disponibilizando os metodos 
 * transacionais que permitem cadastrar usuarios e enviar propostas.
 * 
 * @author positivo
 */
public class DesafioPositivoFacade {

	private static final String EMAIL_CADASTRO_USUARIO_XHTML = "/email/cadastroUsuario.xhtml";

	@In
	private EntityManager entityManager;

	@In(create=true)
	private Renderer renderer;
	
	@In
	private FileUploadBean fileUploadBean;

	/**
	 * Adiciona um usuario no meio de persistencia e realiza uma requisicao ao
	 * servico correspondente da positivo.
	 * 
	 * @param dto
	 *            usuario a ser cadastrado
	 * @throws Exception
	 *             Caso algum problema tenha ocorrido.
	 */
	public void adicionarUsuario(Usuario dto) throws ExcecaoUsuarioCadastrado, ExcecaoEnvioEmail, Exception {
		AutenticacaoSRV autentica = new AutenticacaoSRV(dto.getEmail(), "123456");

		autentica.preparaRequisicao();
		int resp = autentica.requisitaServico().getCodigo();

		switch(CodigoRespostaAutenticacao.fromCodigo(resp)) {
		case CLIENTE_NAO_ENCONTRADO:  
			cadastraNovoUsuario(dto); 
			break; 

		case SENHA_INVALIDA: throw new ExcecaoUsuarioCadastrado();

		case SUCESSO: throw new ExcecaoUsuarioCadastrado();

		default: throw new Exception("Nao foi possivel efetuar o cadastro."); 
		}
	}

	/**
	 * Atualiza os dados do usuario no meio de persistencia e realiza uma requisicao 
	 * ao servico correspondente da Positivo.
	 * 
	 * @param dto
	 * @throws Exception
	 */
	public void atualizarUsuario(Usuario usuarioLogado) throws Exception {
		try {
			Usuario usuario = recuperaUsuario(usuarioLogado.getEmail());
			
			if(usuario == null) {
				throw new Exception("problemas na requisicao.");
			}
			
			usuario.setNome(usuarioLogado.getNome());
			usuario.setSobrenome(usuarioLogado.getSobrenome());
			usuario.setEstado(usuarioLogado.getEstado());
			usuario.setSexo(usuarioLogado.getSexo());
			usuario.setNascimento(usuarioLogado.getNascimento());
			
			AtualizacaoSRV srv = new AtualizacaoSRV(usuario);
			
			srv.preparaRequisicao();
			RespostaPositivo resp = srv.requisitaServico();
			
			switch(CodigoRespostaCadastro.fromCodigo(resp.getCodigo())) {
			 case SUCESSO: 
				 usuario.setToken(resp.getToken()); 
				 entityManager.merge(usuario); 
				 entityManager.flush();
				 break;
			 
			 default: throw new Exception("problemas na requisicao"); 
			}
		}
		catch(Exception e) {
			e.printStackTrace();
			throw new Exception("problemas na requisicao");
		}
	}


	public void alterarSenha(Usuario usuarioLogado, AlteraSenhaDTO senha) throws Exception {
		//verifica se a senha atual informada eh a correta, procedendo 
		//com uma autenticacao.
		AutenticacaoSRV autenticacaoSrv = new AutenticacaoSRV(usuarioLogado.getEmail(), senha.getSenhaAtual());
		
		autenticacaoSrv.preparaRequisicao();
		
		RespostaPositivo resp = autenticacaoSrv.requisitaServico();
		
		switch(CodigoRespostaAutenticacao.fromCodigo(resp.getCodigo())) {
			case SUCESSO:
				realizaAlteracaoSenha(usuarioLogado, senha);
				break;
			case SENHA_INVALIDA: throw new Exception("senha atual invalida");
			
			default: throw new Exception("problemas no processamento da sua requisicao");
		}
	}

	private void realizaAlteracaoSenha(Usuario usuarioLogado, AlteraSenhaDTO senha) throws Exception {
		Usuario usuario = recuperaUsuario(usuarioLogado.getEmail());
		
		NovaSenhaSRV novaSenhaSrv = new NovaSenhaSRV(usuario.getToken(), senha.getNovaSenha());
		
		novaSenhaSrv.preparaRequisicao();
		
		RespostaPositivo resp = novaSenhaSrv.requisitaServico();
		
		switch(CodigoRespostaNovaSenha.fromCodigo(resp.getCodigo())) {
			case SUCESSO: 
				usuario.setToken(resp.getToken());
				entityManager.merge(usuario);
				entityManager.flush();
				break;
			default: throw new Exception("problemas no processamento da requisicai");
		}
	}

	/**
	 * Confirma a solicitacao de cadastro.
	 * @param dto dados da confirmacao de solicitacao de cadastro
	 */
	public void confirmarSolicitacaoCadstro(Usuario dto) throws ExcecaoUsuarioNaoEncontrado, Exception {
		Usuario usuario = recuperaUsuario(dto.getEmail());

		if(usuario != null) {
			usuario.setSenha(dto.getSenha());

			CadastroSRV srv = new CadastroSRV(usuario);
			srv.preparaRequisicao();
			RespostaPositivo resp = srv.requisitaServico();

			switch(CodigoRespostaCadastro.fromCodigo(resp.getCodigo())) {
			case SUCESSO : 
				confirmaCadastro(dto, usuario, resp);
				break;

			case CLIENTE_JA_EXISTE: 
				//existe na positivo, mas continua pendente aqui
				if(usuario.getSituacaoAcessoAtual().getClass().equals(AcessoSolicitado.class)) {
					confirmaCadastro(dto, usuario, resp);
				}
				else {
					throw new ExcecaoUsuarioCadastrado();
				}

			default:  throw new Exception("Nao foi possivel confirmar o cadastro do usuario.");
			}

		}
		else {
			throw new ExcecaoUsuarioNaoEncontrado();
		}
	}

	
	/*
	 * Persiste um novo usuario na base de dados.
	 */
	private void cadastraNovoUsuario(Usuario usuario) throws ExcecaoEnvioEmail, Exception {
		//		try {
		//			renderer.render(EMAIL_CADASTRO_USUARIO_XHTML);
		//		}
		//		catch(Exception e) {
		//			throw new ExcecaoEnvioEmail("Nao foi possivel enviar o email com a solicitacao de cadastro. Tente novamente.");
		//		}

		AcessoSolicitado acesso = new AcessoSolicitado();

		acesso.setUsuario(usuario);
		acesso.setCodigoEfetivacao(geraCodigoConfirmacaoCadastro(usuario));

		usuario.getHistoricoSituacaoAcesso().add(acesso);

		entityManager.merge(usuario);
		entityManager.flush();
	}

	/**
	 * Autentica um usuario requisitando um servico da 
	 * Positivo. 
	 * 
	 * @param email do usuario
	 * @param senha do usuario
	 * @return o usuario autenticado
	 * @throws Exception caso ocorra algum problema na excecao
	 */
	public Usuario autenticarUsuario(String email, String senha) throws ExcecaoFalhaAutenticacao, ExcecaoAcessoUsuario, Exception {
		RespostaPositivo resp = autenticarNaRedePositivo(email, senha);

		Usuario usuario = recuperaUsuario(email);

		if(usuario == null) {
			throw new ExcecaoFalhaAutenticacao("Usuario ou senha invalida.");
		}
		else {
			usuario.getSituacaoAcessoAtual().autenticar(resp.getCodigo() == 0);
		}

		switch (CodigoRespostaAutenticacao.fromCodigo(resp.getCodigo())) {
		case SUCESSO: return recuperaUsuario(email);

		case SENHA_INVALIDA: throw new ExcecaoFalhaAutenticacao("Usuario ou senha invalida");

		case CLIENTE_NAO_ENCONTRADO: throw new ExcecaoFalhaAutenticacao("Usuario ou senha invalida"); 

		default: throw new Exception("Problemas na autenticacao do usuario");
		}
	}

	/**
	 * Adiciona uma proposta submetida pelo usuario logado.
	 * 
	 * @param usuarioLogado usuario logado no sistema
	 * @param proposta proposta submetida
	 */
	public void adicionarProposta(Usuario usuarioLogado, Proposta proposta) {
		if(usuarioLogado.getPropostas() == null) {
			usuarioLogado.setPropostas(new ArrayList<Proposta>());
		}

		usuarioLogado.getPropostas().add(proposta);
		proposta.setArquivoGUI(fileUploadBean.getFiles().get(0).getData());
		proposta.setUsuario(usuarioLogado);

		entityManager.merge(usuarioLogado);
		entityManager.flush();
	}

	/**
	 * Utilizando contexto transacional, recupera as propostas 
	 * submetidas pelo usuario e que podem nao ter sido previamente 
	 * carregadas (uso da propriedade Lazy). 
	 * 
	 * @param usuarioLogado usuario logado no sistema
	 * @return propostas submetidas pelo usuario
	 */
	public List<Proposta> recuperaPropostas(Usuario usuarioLogado) {
		return usuarioLogado.getPropostas();
	}
	
	/*
	 * Confirma o cadastro do usuario na base de dados local.
	 */
	private void confirmaCadastro(Usuario dto, Usuario usuario, RespostaPositivo resp) throws ExcecaoAcessoUsuario {
		usuario.setToken(resp.getToken());
		usuario.getSituacaoAcessoAtual().confirmarCadastro(dto.getCodigoConfirmacaoCadastro());
		entityManager.merge(usuario);
		entityManager.flush();
	}

	/*
	 * Realiza a autenticacao na rede Positivo.
	 */
	private RespostaPositivo autenticarNaRedePositivo(String email, String senha) throws Exception {
		AutenticacaoSRV req = new AutenticacaoSRV(email, senha);
		req.preparaRequisicao();
		RespostaPositivo resp = req.requisitaServico();
		return resp;
	}
	
	/*
	 * Gera um codigo para ser usado na confirmacao do cadastro.
	 */
	private String geraCodigoConfirmacaoCadastro(Usuario usuario) throws Exception {
		Date dataAtual = Calendar.getInstance().getTime();	
		String codigo = CriptografiaUtil.criptografarMD5(usuario.getEmail() + dataAtual.toString());
		return codigo;
	}
	
	/*
	 * Gera uma nova senha para o usuario, utilizando um hash.  
	 */ 
	private String geraSenha(Usuario dto) throws java.lang.Exception {
		String email = dto.getEmail();
		String date = Calendar.getInstance().getTime().toString();
		
		String senha = CriptografiaUtil.criptografarMD5(email + date).substring(0, 6);
		
		return senha.toUpperCase();
	}
	
	/*
	 * Recupera um usuario na base de dados pelo email.
	 * Nessa arquitetura, optamos por nao fazer uso do padrao
	 * Data Access Objects, dada a simplicidade do projeto. 
	 */
	private Usuario recuperaUsuario(String email) throws Exception {
		try {
			@SuppressWarnings("unchecked")
			List<Usuario> usuarios = entityManager.createQuery(
					"FROM Usuario u where u.email = :pEmail").setParameter(
							"pEmail", email).getResultList();

			if(usuarios == null || usuarios.size() == 0) {
				return null;
			}
			else return usuarios.get(0);
		}
		catch (Exception e) {
			throw new Exception("Problemas na consulta ao usuario");
		}
	}


	/**
	 * Recupera a senha do usuario, fazendo uma requisicao ao 
	 * servico da Positivo para atualizacao de senha. 
	 */
	public void recuperarSenha(Usuario dto) throws ExcecaoAcessoUsuario, ExcecaoUsuarioNaoEncontrado, Exception {
		Usuario usuario = recuperaUsuario(dto.getEmail());
		
		if(usuario != null) {
			usuario.getSituacaoAcessoAtual().alterarSenha();
			
			NovaSenhaSRV novaSenha = new NovaSenhaSRV(usuario.getToken(), geraSenha(dto));
			novaSenha.preparaRequisicao();
			
			RespostaPositivo resp = novaSenha.requisitaServico();

			switch(CodigoRespostaNovaSenha.fromCodigo(resp.getCodigo())) {
			  case SUCESSO: 
				  //TODO: enviar o email com a senha
				  usuario.setToken(resp.getToken());
				  entityManager.merge(usuario);
				  entityManager.flush();
				  break;
			  
			  case SENHA_INVALIDA : throw new Exception("Senha gerada invalida. Tente novamente");
			  
			  case CLIENTE_NAO_EXISTE: throw new ExcecaoUsuarioNaoEncontrado();
			  
			  case OUTROS: throw new Exception("Nao foi possivel realizar a transacao.");
			}
		} else {
			throw new ExcecaoUsuarioNaoEncontrado();
		}
	}

}
