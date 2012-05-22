package br.unb.cdt.desafioPositivo.facade;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;

import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;

import br.unb.cdt.desafioPositivo.model.Proposta;
import br.unb.cdt.desafioPositivo.model.Usuario;
import br.unb.cdt.desafioPositivo.util.rest.AutenticacaoSRV;
import br.unb.cdt.desafioPositivo.util.rest.CadastroSRV;
import br.unb.cdt.desafioPositivo.util.rest.RespostaPositivo;

@Name("facade")
@AutoCreate
public class DesafioPositivoFacade {

	@In
	private EntityManager entityManager;

	/**
	 * Adiciona um usuario no meio de persistencia e realiza uma requisicao ao
	 * servico correspondente da positivo.
	 * 
	 * @param usuario
	 *            usuario a ser cadastrado
	 * @throws Exception
	 *             Caso algum problema tenha ocorrido.
	 */
	public void adicionarUsuario(Usuario usuario) throws ExcecaoUsuarioCadastrado, Exception {
		CadastroSRV req = new CadastroSRV(usuario);

		req.preparaRequisicao();

		RespostaPositivo resp = req.requisitaServico();
	
		switch(resp.getCodigo()) {
	     case 0: persistirUsuario(usuario); break;
	     case 4: throw new ExcecaoUsuarioCadastrado();
	     default: throw new Exception("Problemas na inclusao do usuario.");
		}
	}

	/*
	 * Persiste um novo usuario na base de dados.
	 */
	private void persistirUsuario(Usuario usuario) {
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
	public Usuario autenticarUsuario(String email, String senha) throws Exception {
		AutenticacaoSRV req = new AutenticacaoSRV(email, senha);

		req.preparaRequisicao();

		RespostaPositivo resp = req.requisitaServico();

		switch (resp.getCodigo()) {
		 case 0:return recuperaUsuario(email);
		 case 2: throw new Exception("Senha nao confere");
		 case 3: throw new Exception("Cliente nao encontrado");
		 default : throw new Exception("Problemas na autenticacao do usuario");
		}
	}

	/*
	 * Recupera um usuario na base de dados pelo email.
	 * Nessa arquitetura, optamos por nao fazer uso do padrao
	 * Data Access Objects, dada a simplicidade do projeto. 
	 */
	private Usuario recuperaUsuario(String email) throws Exception {
		try {
			return (Usuario) entityManager.createQuery(
					"FROM Usuario u where u.email = :pEmail").setParameter(
					"pEmail", email).getSingleResult();
		} catch (Exception e) {
			throw new Exception("Problemas na consulta ao usuario");
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
}
